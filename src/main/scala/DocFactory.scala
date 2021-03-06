import scala.tools.nsc._
import scala.tools.nsc.doc.Universe
import scala.tools.nsc.doc.model._
import scala.tools.nsc.doc.model.diagram.DiagramFactory
import scala.tools.nsc.reporters.Reporter

/** A documentation processor controls the process of generating Scala documentation, which is as follows.
  *
  * * A simplified compiler instance (with only the front-end phases enabled) is created, and additional
  * ''sourceless'' comments are registered.
  * * Documentable files are compiled, thereby filling the compiler's symbol table.
  * * A documentation model is extracted from the post-compilation symbol table.
  * * A generator is used to transform the model into the correct final format (HTML).
  *
  * A processor contains a single compiler instantiated from the processor's `settings`. Each call to `document`
  * uses the same compiler instance with the same symbol table. In particular, this implies that the scaladoc site
  * obtained from a call to `run` will contain documentation about files compiled during previous calls to the same
  * processor's `run` method.
  *
  * @param reporter The reporter to which both documentation and compilation errors will be reported.
  * @param settings The settings to be used by the documenter and compiler for generating documentation.
  *
  * @author Gilles Dubochet */
class DocFactory(val reporter: Reporter, val settings: doc.Settings) {
  processor =>

  /** The unique compiler instance used by this processor and constructed from its `settings`. */
  object compiler extends Global(settings, reporter) {
    override protected def computeInternalPhases() {
      phasesSet += syntaxAnalyzer
      phasesSet += analyzer.namerFactory
      phasesSet += analyzer.packageObjects
      phasesSet += analyzer.typerFactory
      phasesSet += superAccessors
      phasesSet += pickler
    }
  }

  /** Creates a scaladoc site for all symbols defined in this call's `files`, as well as those defined in `files` of
    * previous calls to the same processor.
    * @param files The list of paths (relative to the compiler's source path, or absolute) of files to document. */
  def document(files: List[String], out : (AnyRef)=>Unit ): Unit = {
    new compiler.Run() compile files
    //    compiler.addSourceless
    if (!reporter.hasErrors) {
      val modelFactory = new ModelFactory(compiler, settings)
        with CommentFactory
        with ModelFactoryImplicitSupport
        with ModelFactoryTypeSupport
        with DiagramFactory
        with TreeFactory
        with MemberLookup
      val docModel: Option[Universe] = modelFactory.makeModel
      docModel.foreach{
        univ =>
          val root = univ.rootPackage
          def iterate: (Entity) => Unit = {
            case pack: Package =>
              out(pack.name)
              pack.packages.foreach {
                (p: Package) =>
                  iterate(p)
              }

          }
          iterate(root)
      }
      println("model contains " + modelFactory.templatesCount + " documentable templates")

    }
  }

}
